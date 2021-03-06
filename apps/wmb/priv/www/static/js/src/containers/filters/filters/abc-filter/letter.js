import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import { ProgressBar } from 'react-toolbox';
import LetterView from '../../../../view/filters/filters/abc-filter/letter';
import ArtistContainer from './artist';
import { fetchArtistsByLetter } from '../../../../actions';
import { getArtistsByLetter, getActiveArtist } from '../../../../selectors';


// import debugRender from 'react-render-debugger';
// @debugRender
class LetterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    handleUnfoldLetter = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.setState({ folded: !this.state.folded });
        if (!this.props.artists) {
            this.props.fetchArtistsByLetter();
        }
    }

    get artistList() {
        if (this.state.folded) return null;
        if (_.isEmpty(this.props.artists)) {
            return (
                <ProgressBar
                    type='circular'
                    mode='indeterminate'
                    multicolor
                />
            );
        }
        const list = _(this.props.artists)
            .sortBy('artist')
            .map(artistObj =>
                <ArtistContainer
                    isActive = {artistObj.artistId === this.props.activeArtist}
                    key      = {artistObj.artistId}
                    {...artistObj}
                />
            )
            .value();
        return <div>{ list }</div>;
    }

    render() {
        return (
            <LetterView
                active      = {!this.state.folded}
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

export default connect(mapStateToProps, mapDispatchToProps)(LetterContainer);
