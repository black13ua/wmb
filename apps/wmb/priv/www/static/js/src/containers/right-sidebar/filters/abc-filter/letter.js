import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import CSSModules from 'react-css-modules';
import { badge, listGroup } from 'bootstrap-css';

import LetterView from '../../../../view/right-sidebar/filters/abc-filter/letter';
import ArtistContainer from './artist';
import { fetchArtistsByLetter } from '../../../../actions';
import { getArtistsByLetter, getActiveArtist } from '../../../../selectors';

const styles = {};
Object.assign(styles, badge, listGroup);


class LetterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    handleUnfoldLetter = (event) => {
        console.info('handleUnfoldLetter');
        event.preventDefault();
        event.stopPropagation();
        this.setState({ folded: !this.state.folded });
        if (!this.props.artists) {
            this.props.fetchArtistsByLetter();
        }
    }

    get artistList() {
        if (this.state.folded || _.isEmpty(this.props.artists)) return null;

        const list = this.props.artists.map(artistObj =>
            <ArtistContainer
                key      = {artistObj.artistId}
                isActive = {artistObj.artistId === this.props.activeArtist}
                {...artistObj}
            />
        );
        return (
            <div styleName="list-group">
                { list }
            </div>
        );
    }

    render() {
        return (
            <LetterView
                artistCount = {_.size(this.props.artists)}
                letter      = {this.props.letter}
                onClick     = {this.handleUnfoldLetter}
            >
                {this.artistList}
            </LetterView>
        );
    }
}


LetterContainer.propTypes = {
    activeArtist        : PropTypes.number.isRequired,
    artists             : PropTypes.array,
    fetchArtistsByLetter: PropTypes.func.isRequired,
    letter              : PropTypes.string.isRequired,
};

const mapStateToProps = (state, props) => ({
    activeArtist: getActiveArtist(state, props),
    artists     : getArtistsByLetter(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchArtistsByLetter: ()    => dispatch(fetchArtistsByLetter(ownProps.letterId)),
});

const styledLetterContainer = CSSModules(LetterContainer, styles, { allowMultiple: true });
export default connect(mapStateToProps, mapDispatchToProps)(styledLetterContainer);